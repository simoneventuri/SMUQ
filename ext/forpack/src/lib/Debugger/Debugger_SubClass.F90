SubModule(Debugger_Class) Debugger_SubClass

  use Logger_Class              ,only:  Logger, LogLevel_NOLOGS, LogLevel_HEAVYDEBUG

  implicit none

  contains

Module Procedure InitializeDebugger

  use Input_Library             ,only:  InputParameter_Type
  use String_Library            ,only:  Parse, RemoveQuotes, ParseFunction, IsFunction, UpperCase, EmptyString
  use DebugVariable_Class       ,only:  NewDebugVariable
  use File_Library              ,only:  GetBaseName, AddFileSuffix
  use ,intrinsic :: iso_fortran_env      ,only:  Output_Unit

  character(*)                                              ,parameter  ::  ProcName='InitializeDebugger'
  character(*)                                              ,parameter  ::  DefaultFileName = "debuggerXX.dbg"
  logical                                                               ::  UnitAdded, LoggerUnit
  integer                                                               ::  iParam, iItem, iArg, iVar
  character(:)  ,allocatable                                            ::  String, ListVar(:), Strings(:), Name, Value
  character(:)  ,allocatable                                            ::  FctName, ArgNames(:), ArgValues(:), FileName
  type(InputParameter_Type)                                             ::  Param

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_HEAVYDEBUG )

  This%NItems   =   0
  allocate( This%DbgItems(This%NItems) )

  do iParam = 1,Input%GetNumberOfParameters()

    String    =   Input%GetRawValue(iParam)
    call Logger%Write( "-> iParam = ", iParam, "String = ", String )

    if ( .Not. IsFunction(String,"Debug") ) cycle
    call ParseFunction( String, FctName, ArgNames, ArgValues, DefArgNames=["Var"] )

    call This%AddItem()
    iItem   =   This%NItems
    associate( Item => This%DbgItems(iItem) )

      UnitAdded   =   .False.
      LoggerUnit  =   .False.

      do iArg = 1,size(ArgNames)
        Name   =   UpperCase(trim(ArgNames(iArg)))
        Value  =   trim(ArgValues(iArg))

        select case (Name)

          case ("VAR")
            call Parse( Value, ",", ListVar, IgnoreBetween=["'",'"'] )
            do iVar = 1,size(ListVar)
              String    =   trim(ListVar(iVar))
              call Logger%Write( "    -> iVar = ", iVar, "String = ", String )
              call Parse( String, "=", Strings )
              if ( size(Strings) < 2 ) cycle
              Name  =   trim(Strings(1))
              Value =   trim(RemoveQuotes(Strings(2)))
              call Logger%Write( "    -> Calling Param%Initialize: Name = ", Name, "Value = ", Value )
              call Param%Initialize( Name, Value )
              call Item%AddVar( Var=NewDebugVariable(Param) )
            end do

          case ("ONLY")
            call Parse( Value, ",", ListVar )
            Item%IncludedComponentList  =   ListVar

          case ("EXCLUDE")
            call Parse( Value, ",", ListVar )
            Item%ExcludedComponentList  =   ListVar

          case ("TOLOGGER")
            LoggerUnit    =   .True.
!             FileName    =   Logger%GetFileName(1)
!             This%Units(k)%
!             call Logger%Write( "    -> FileName = ", FileName )
!             call This%AddUnit( FileName )
!             UnitAdded   =   .True.


          case ("FILE")
            FileName    =   Value
            call Logger%Write( "    -> FileName = ", FileName )
            call This%AddUnit( FileName )
            UnitAdded   =   .True.

          case ("FILESUFFIX")
            FileName  =   DefaultFileName
            if ( Logger%GetUnit() /= Output_Unit ) FileName = Logger%GetFileName(1,AbsolutePath=.True.)
  !           FileName    =   Logger%GetFileName(1,AbsolutePath=.True.)
  !           if ( len_trim(FileName) == 0 ) FileName = DefaultFileName ! This does not work since the StdOut file name is <STDOUT>
            FileName  =   AddFileSuffix( FileName, Value, Separator="." )
            call Logger%Write( "    -> FileName = ", FileName )
            call This%AddUnit( FileName )
            UnitAdded   =   .True.

        end select

      end do

      if ( .Not. UnitAdded ) call This%AddUnit("")
      Item%iLogUnit =   iItem
      This%Units(iItem)%Active      =   .False.
!       if ( .Not. allocated(Item%IncludedComponentList) ) call EmptyString( Item%IncludedComponentList )
!       if ( .Not. allocated(Item%ExcludedComponentList) ) call EmptyString( Item%ExcludedComponentList )


      if ( LoggerUnit ) then
        call Logger%Write( "Setting debugger unit tot Logger unit (still experimental)" )
        This%Units(iItem)   =   Logger%Units(1)
      end if

    end associate

  end do

  call Logger%Exiting()

End Procedure

! This procedure adds an Item to the list of DebuggerItems objects.
Module Procedure AddDebuggerItem
  use DebuggerItem_Class    ,only:  NewDebuggerItem
  integer                                                               ::  i
  type(DebuggerItem_Type) ,allocatable                                  ::  List(:)
  type(DebuggerItem_Type)                                               ::  Item
  if ( .Not. allocated(This%DbgItems) ) allocate( This%DbgItems(0) )
  Item  =   NewDebuggerItem(                &
              Var         =   Var         , &
              Vars        =   Vars        , &
              Components  =   Components  , &
              LogLevel    =   LogLevel      )
  allocate( List, source = [This%DbgItems,Item]  )
  call move_alloc( List, This%DbgItems )
  This%NItems =   size(This%DbgItems)
End Procedure

























Module Procedure OutputDebugger
  character(*)                                              ,parameter  ::  ProcName='OutputDebugger'
  integer                                                               ::  i
  call Logger%Entering( ProcName )
  if ( This%NItems == 0 ) then
    call Logger%Write( "No data in Debugger" )
  else
    call Logger%Write( "Debugger Data:" )
    do i = 1,This%NItems
      call This%DbgItems(i)%Output()
    end do
  end if
  call Logger%Exiting()
End Procedure

Module Procedure ActiveDebugger
  integer                                                               ::  i

  integer         ,allocatable                                          ::  ItemsIndex(:)
  Active    =   .False.

  if ( Label == "" ) then
    allocate( ItemsIndex, source = [(i,i=1,size(This%DbgItems))] )
  else
    call GetItemListToProcess( This, ItemsIndex, Label )
  end if

  do i = 1,size(ItemsIndex)
  associate( Item => This%DbgItems(ItemsIndex(i)) )
!     call Logger%Write( "    [ActiveDebugger] -> i = ", i, "ItemsIndex(i) = ", ItemsIndex(i), "Item%iLogUnit = ", Item%iLogUnit, Debug=.True. )
    Active  =   Item%Active(This%Logger_Type)   !@TODO: Wrong when there are several labels
!     call Logger%Write( "    [ActiveDebugger] -> Active = ", Active, Debug=.True. )
    if ( Active ) return
  end associate
  end do

End Procedure

Module Procedure LoggerActive
  IsActive  =   This%ActiveDebugger("")
End Procedure

! This procedure set the conditions for which the Debugger should be active
Module Procedure SetDebuggerConditions
!   if ( DebuggerItemExists(This,Label) ) then
!   else
!     call This%AddItem( Var, Vars, Label, LogLevel )
!   end if
End Procedure


Module Procedure RemoveDebuggerItem
  integer                                                               ::  iItem, i
  type(DebuggerItem_Type) ,allocatable                                  ::  List(:)
  if ( .Not. allocated(This%DbgItems) ) return
  if ( size(This%DbgItems) == 0 ) return
  iItem   =   This%GetItemIndex( Label )
  if ( iItem == 0 ) return
  allocate( List, source = pack(This%DbgItems,Mask=[(i/=iItem,i=1,size(This%DbgItems))] ) )
  call move_alloc( List, This%DbgItems )
  This%NItems    =   size(This%DbgItems)
End Procedure



Pure Function DebuggerItemExists( This, Label ) result(Exist)
  type(Debugger_Type)                                   ,intent(in)     ::  This
  character(*)                                          ,intent(in)     ::  Label
  logical                                                               ::  Exist
  Exist   =   .False.
End Function

Module Procedure GetDebuggerItemIndexFromItemName
  use String_Library            ,only:  Equal
  integer                                                               ::  i
  iItem   =   0
  do i = 1,This%NItems
    if ( .Not. Equal(Label,This%DbgItems(i)%GetLabel(),CaseSensitive=.False.) ) cycle
    iItem =   i
    return
  end do
End Procedure











! This procedure assess whether the Debugger should be active for the current conditions
Module Procedure UpdateDebuggerStatus
  integer                                                               ::  i, iItem
  integer         ,allocatable                                          ::  ItemsIndex(:)
  if ( .Not. allocated(This%DbgItems) ) return
  if ( size(This%DbgItems) == 0 ) return
!   call Logger%Entering( "UpdateDebuggerStatus" )
!   call Logger%Write( "Updating Debugger status for: Name = ", Name, "Value = ", Value )
  call GetItemListToProcess( This, ItemsIndex, Component )
!   call Logger%Write( "-> size(ItemsIndex) = ", size(ItemsIndex) )
  do i = 1,size(ItemsIndex)
    iItem   =   ItemsIndex(i)
    associate( Item => This%DbgItems(iItem) )
!       call Logger%Write( "-> Calling Item%UpdateStatus: i = ", i, "iItem = ", iItem )
      call Item%UpdateStatus( Name, Value, This%Components, This%Logger_Type )
!       call Logger%Write( "-> This%Logger_Type%Units(iItem)%Active = ", This%Logger_Type%Units(iItem)%Active )
    end associate
  end do
!   call Logger%Exiting()
End Procedure


Module Procedure ConditionsSatisfied_All
  integer                                                               ::  i, iItem
  integer         ,allocatable                                          ::  ItemsIndex(:)
  Ok  =   .False.
  if ( .Not. allocated(This%DbgItems) ) return
  if ( size(This%DbgItems) == 0 ) return
  call GetItemListToProcess( This, ItemsIndex, Label )
  do i = 1,size(ItemsIndex)
  associate( Item => This%DbgItems(ItemsIndex(i)) )
    Ok  =   Item%ConditionsOk( Name, Value )
    if ( Ok ) exit
  end associate
  end do
End Procedure

! This procedure return the set of accepted values for a given variable associated to a given Label.
! The accepted values are the values for which the variable VarName will yeilds activation of the
! Debugger.
Module Procedure GetDebuggerIntVarConditionsFromLabelName
  integer                                                               ::  iItem, iVar
  allocate( VarValues(0) )
  if ( .Not. allocated(This%DbgItems) ) return
  if ( size(This%DbgItems) == 0 ) return
  iItem   =   This%GetItemIndex( Label )
  if ( iItem == 0 ) return
  iVar    =  This%DbgItems(iItem)%GetVariableIndex(VarName)
  if ( iVar == 0 ) return
  if ( .Not. allocated(This%DbgItems(iItem)%Var(iVar)%iValues) ) return
  VarValues   =   This%DbgItems(iItem)%Var(iVar)%iValues
End Procedure

Module Procedure GetDebuggerVarConditions

  use String_Library      ,only:  EmptyString, Inline
  use Utilities_Library   ,only:  AddElementToArray

  integer                                                               ::  i, iVar
  integer         ,allocatable                                          ::  ItemsIndex(:)
  character(:)    ,allocatable                                          ::  String

  call EmptyString( Strings )

  if ( .Not. allocated(This%DbgItems) ) return
  if ( size(This%DbgItems) == 0 ) return

  call GetItemListToProcess( This, ItemsIndex, Label )
  if ( size(ItemsIndex) == 0 ) return

  do i = 1,size(ItemsIndex)
  associate( Item => This%DbgItems(ItemsIndex(i)) )

    iVar    =   Item%GetVariableIndex(VarName)
    if ( iVar == 0 ) cycle

    String  =   ""
    associate( Var => Item%Var(iVar) )
      if ( allocated(Var%iValues) ) then
        if ( size(Var%iValues) > 0 ) then
          String = Inline( Var%iValues, Separator=",")
        end if
      else if ( allocated(Var%cValues) ) then
        if ( size(Var%cValues) > 0 ) then
          if ( len_trim(String) /= 0 ) String = String //","
          String = String // Inline( Var%cValues, Separator=",")
        end if
      end if
    end associate
    if ( len_trim(String) == 0 ) cycle
    call AddElementToArray( String, Strings )

  end associate
  end do

End Procedure

! This procedure returns a list of indexes associated to the DebuggerItem
! objects to be processed. The returned list of indexes depends on the
! presence of the the 'Component' optional argument:
! * If absent, then all the DebuggerItems will be considered.
! * If present, then only the DebuggerItem object whose name match the
!   value stored in 'Component' will be returned. If no match is found,
!   then an empty array is returned.
! Also, if a Component is specified, but an actual Item has no Component (empty string),
! then it should be included. So that, when a variable is declared w/o
! any Component, as in:
!     iRea='1 4'
! it will be appled to all Items
! This is to be compared to the definition:
!     : iRea='1 4', Component='Kinetic'
! which will be applied only for Component='Kinetic'
Subroutine GetItemListToProcess( This, ItemsIndex, Component )
  use Utilities_Library   ,only:  AddElementToArray
  class(Debugger_Type)                                  ,intent(in)     ::  This
  integer         ,allocatable                          ,intent(out)    ::  ItemsIndex(:)
  character(*)                                ,optional ,intent(in)     ::  Component
  integer                                                               ::  iItem
  if ( present(Component) ) then
    iItem   =   This%GetItemIndex( Component )
    if ( iItem == 0 ) then
      allocate( ItemsIndex(0) )
    else
      allocate( ItemsIndex, source = [iItem] )
    end if
    do iItem = 1,size(This%DbgItems)
      if ( len_trim(This%DbgItems(iItem)%GetLabel()) > 0 ) cycle
!       if ( size(This%IncludedComponentList) > 0 ) then
!       end if
      call AddElementToArray( iItem, ItemsIndex ) ! Add all Items which are nameless
    end do
  else
    allocate( ItemsIndex, source = [(iItem,iItem=1,size(This%DbgItems))] )
  end if
End Subroutine
































!
! Module Procedure FindComponentIndex
!   use String_Library            ,only:  Equal
!   integer                                                               ::  i
!   Idx   =   0
!   if ( .Not. allocated(This%Components) ) return
!   do i = 1,size(This%Components)
!     if ( .Not. Equal(Component,This%Components(i)%Name,CaseSensitive=.False.,Trimed=.True.) ) cycle
!     Idx =   i
!     return
!   end do
! End Procedure
!
! ! This procedure adds a Probe to the list of Probe objects.
! Module Procedure AddComponemt
!   integer                                                               ::  N, i
!   type(DbgComponent_Type)   ,allocatable                                ::  List(:)
!   type(DbgComponent_Type)                                               ::  Item
!   if ( .Not. allocated(This%Components) ) allocate( This%Components(0) )
!   This%NComponents    =   size(This%Components)
!   N    =   size(This%Components)
!   allocate( List(N+1) )
!   do i = 1,N
!     List(i)   =   This%Components(i)
!   end do
!   Item%Name   =   Name
!   Item%Active =   .True.
!   List(N+1)   =   Item
!   call move_alloc( List, This%Components )
!   This%NComponents    =   size(This%Components)
! End Procedure
!
! ! Module Procedure RemoveComponentFromIndex
! !   logical ,dimension(This%NProbes)                                      ::  ToKeep
! !   type(DbgComponent_Type)   ,allocatable                                ::  List(:)
! !   if ( (This%NProbes <= 0) .or. (.Not. allocated(This%Components)) ) then
! !     call Logger%Exiting()
! !     return
! !   end if
! !   if ( Index <= 0 .or. Index > This%NProbes ) then
! !     call Logger%Exiting()
! !     return
! !   end if
! !   ToKeep        =   .True.
! !   ToKeep(Index) =   .False.
! !   allocate( List , source = pack(This%Components,ToKeep) )
! !   call move_alloc( List, This%Components )
! !   This%NProbes    =   size(This%Components)
! ! End Procedure




! This procedure adds a component to from its name.
Module Procedure AddComponentToDebuggerFromName
  call This%Components%Add( ComponentName )
End Procedure

! This procedure removes a component to from its name.
Module Procedure RemoveComponentFromDebuggerFromName
  call This%Components%Remove( ComponentName )
End Procedure
!
!
! This procedure adds a component to the list of components currently active
Module Procedure ActivateComponent
  call This%Components%Activate( ComponentNames )
End Procedure

Module Procedure DeactivateComponent
  call This%Components%Deactivate( ComponentNames )
End Procedure

Module Procedure GetComponentList
  ComponentNames    =   This%Components%GetNames()
End Procedure



End SubModule